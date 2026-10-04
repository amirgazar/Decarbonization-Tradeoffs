# Purpose:
# Purpose: Submit an ARC job that compresses final summaries and copies them directly to Google Drive.
# Open in LOCAL RStudio and Source. Choose 1 for one-time cloud setup instructions, then 2 to transfer.
# Credentials remain in rclone's configuration on ARC; never paste tokens into this script.
SSH_HOST <- 'amirgazar@tinkercliffs2.arc.vt.edu'
CLOUD_REMOTE <- 'phased_cloud'
CLOUD_FOLDER <- 'PHASED_1000_results'
DRIVE_FOLDER_ID <- '1nL_XXIYFaLI6u16iQXE0GpzGC9Lhx0Wj'
REPOSITORY <- Sys.getenv("PHASED_R1_ROOT", unset=getwd())
RUN_NAME <- 'R1_ensemble101_1000_20260920_01'
ARC_FOLDER <- file.path(REPOSITORY,'2 Generation Expansion Model/5 Dispatch Curve/2 Advanced Research Computing/1 ARC Codes')
RECEIPT_FOLDER <- file.path(ARC_FOLDER,'Logs R1',RUN_NAME,'Cloud transfer R1')
cloud_setup_R1<-function(){
 cat('\nONE-TIME SETUP ON ARC\n1. Open https://ood.arc.vt.edu and start Remote Desktop.\n2. In a terminal inside that desktop run:\n   module load rclone\n   rclone config\n3. Choose n (new remote), name it phased_cloud.\n4. Choose Google Drive (drive).\n   Leave client ID/secret blank unless your institution provides them.\n   Choose drive scope to access the existing destination folder (Google will show the requested permissions).
   Set root_folder_id to 1nL_XXIYFaLI6u16iQXE0GpzGC9Lhx0Wj in advanced configuration.\n5. Use browser authentication on that ARC desktop, sign in to your chosen account,\n   select your Google account, and save the remote. Do not share the token/configuration.\n6. Return here, Source this script and choose 2.\n\n')
}
cloud_python_R1<-function()paste(c(
 'import pathlib,sys,hashlib,json,zipfile',
 'root=pathlib.Path(sys.argv[1]); out=pathlib.Path(sys.argv[2]); out.mkdir(parents=True,exist_ok=True)',
 'names=["Yearly_Results.csv","Yearly_Results_Shortages.csv","Yearly_Facility_Level_Results.csv","Coverage_and_accounting.csv","SUMMARY_COMPLETE.txt"]',
 'for n in names:',
 ' if not (root/n).is_file() or (root/n).is_symlink(): raise RuntimeError("Missing or invalid summary: "+n)',
 'def sha(p):',
 ' h=hashlib.sha256()',
 ' with p.open("rb") as f:',
 '  for b in iter(lambda:f.read(8*1024*1024),b""): h.update(b)',
 ' return h.hexdigest()',
 'before={n:sha(root/n) for n in names}',
 'archive=out/"PHASED_1000_summaries.zip"',
 'with zipfile.ZipFile(archive,"w",compression=zipfile.ZIP_DEFLATED,compresslevel=1,allowZip64=True) as z:',
 ' for n in names: z.write(root/n,arcname="Final/"+n)',
 'if before!={n:sha(root/n) for n in names}: raise RuntimeError("Source changed during compression")',
 '(out/"Source_checksums.json").write_text(json.dumps(before,indent=2))',
 '(out/"Archive_SHA256.txt").write_text(sha(archive)+"  "+archive.name+"\\n")',
 'print("Compressed archive bytes:",archive.stat().st_size)',
 'print("Original summaries unchanged; no simulation partitions included.")'
),collapse='\n')
cloud_disconnect_R1<-function(session)ssh::ssh_disconnect(session)
cloud_upload_R1<-function(session,files,to)ssh::scp_upload(session,files,to=to)
cloud_connect_R1<-function(){if(!requireNamespace('ssh',quietly=TRUE))stop('Run install.packages("ssh") first');ssh::ssh_connect(SSH_HOST)}
cloud_exec_R1<-function(session,command){r<-ssh::ssh_exec_internal(session,command,error=FALSE);if(r$status!=0L)stop(rawToChar(r$stderr));trimws(rawToChar(r$stdout))}
cloud_submit_R1<-function(){
 if(!grepl('^[A-Za-z][A-Za-z0-9_-]*$',CLOUD_REMOTE)||grepl('[\r\n]',CLOUD_FOLDER)||!nzchar(CLOUD_FOLDER))stop('Invalid cloud destination')
 receipt<-file.path(RECEIPT_FOLDER,'Latest_transfer.rds')
 if(file.exists(receipt))stop('A transfer attempt is already recorded. Choose 3 to inspect it before starting another.')
 p<-file.path(ARC_FOLDER,'Generated R1',RUN_NAME,'bundle R1');e<-new.env();sys.source(file.path(p,'1 ARC Settings_R1.R'),e);cfg<-e$arc
 session<-cloud_connect_R1();on.exit(cloud_disconnect_R1(session));q<-function(x)shQuote(x,type='sh')
 available<-cloud_exec_R1(session,"bash -lc 'module load rclone && rclone listremotes'")
 if(!paste0(CLOUD_REMOTE,':')%in%strsplit(available,'\n',fixed=TRUE)[[1]])stop('Cloud remote not configured. Choose 1 for setup instructions.')
 final<-file.path(cfg$results_root,'Backup summary 1000 R1','Final')
 cloud_exec_R1(session,paste('test -f',q(file.path(final,'SUMMARY_COMPLETE.txt'))))
 stamp<-format(Sys.time(),'%Y%m%d_%H%M%S');remote<-file.path(cfg$code_root,'Cloud transfer R1',stamp)
 dest<-paste0(CLOUD_REMOTE,':',sub('/+$','',CLOUD_FOLDER),'/',stamp)
 local<-file.path(RECEIPT_FOLDER,stamp);dir.create(local,recursive=TRUE,showWarnings=FALSE)
 cloud_exec_R1(session,paste('bash -lc',q(paste('module load rclone && rclone lsf',q(paste0(CLOUD_REMOTE,':')),'--drive-root-folder-id',q(DRIVE_FOLDER_ID),'--max-depth 1'))))
 py<-file.path(local,'Pack_summaries_R1.py');writeLines(cloud_python_R1(),py)
 shell<-readLines(file.path(p,'5_BASH_Summary_ensemble_R1.sh'))
 shell<-gsub('summary-ensemble_R1','cloud-transfer-1000',shell,fixed=TRUE)
 shell<-sub('^#SBATCH --cpus-per-task=.*','#SBATCH --cpus-per-task=1',shell)
 shell<-sub('^#SBATCH --mem=.*','#SBATCH --mem=4G',shell)
 shell<-sub('^#SBATCH --time=.*','#SBATCH --time=0-04:00:00',shell)
 shell<-head(shell,-1L)
 shell<-c(shell,'umask 077','module load rclone',paste('python3',q(file.path(remote,basename(py))),q(final),q(file.path(remote,'archive'))),
  paste('rclone copy',q(file.path(remote,'archive')),q(dest),'--drive-root-folder-id',q(DRIVE_FOLDER_ID),'--immutable --transfers 2 --retries 3 --stats 30s'),
  paste('rclone check',q(file.path(remote,'archive')),q(dest),'--drive-root-folder-id',q(DRIVE_FOLDER_ID),'--one-way --download'),
  paste('printf',q('%s\n'),q(paste('Verified cloud transfer complete:',dest))))
 script<-file.path(local,'Cloud_transfer_R1.sh');writeLines(shell,script)
 cloud_exec_R1(session,paste('mkdir -p',q(remote)))
 cloud_upload_R1(session,c(py,script),remote)
 state<-list(status='submission_pending',job='',remote=remote,destination=dest)
 saveRDS(state,receipt)
 id<-cloud_exec_R1(session,paste('cd',q(remote),'&& sbatch --parsable',q(basename(script))))
 id<-sub(';.*$','',id);if(!grepl('^[0-9]+$',id))stop('Uncertain submission response; choose 3 before retrying')
 state$status<-'submitted';state$job<-id;saveRDS(state,receipt)
 cat('ARC transfer job:',id,'\nCloud destination:',dest,'\nThe upload and verification now run on ARC. Your Mac does not need to stay connected.\n')
}
cloud_status_R1<-function(){
 receipt<-file.path(RECEIPT_FOLDER,'Latest_transfer.rds');if(!file.exists(receipt)){cat('No transfer recorded.\n');return(invisible(NULL))}
 state<-readRDS(receipt);cat('Destination:',state$destination,'\n')
 if(!nzchar(state$job)){cat('Submission uncertain. Inspect ARC jobs and logs in:',state$remote,'\nDo not submit a duplicate.\n');return(invisible(NULL))}
 session<-cloud_connect_R1();on.exit(cloud_disconnect_R1(session))
 cat(cloud_exec_R1(session,paste('sacct -j',state$job,'--format=JobID,State,Elapsed,ExitCode --parsable2')),'\n')
 for(ext in c('out','err')){
  path<-file.path(state$remote,paste0('cloud-transfer-1000_',state$job,'.',ext))
  r<-ssh::ssh_exec_internal(session,paste('test -f',shQuote(path,type='sh'),'&& tail -n 30',shQuote(path,type='sh')),error=FALSE)
  if(r$status==0L)cat(rawToChar(r$stdout),'\n')
 }
}
if(!isTRUE(getOption('phased.cloud.no_run',FALSE))){
 choice<-utils::menu(c('One-time Google Drive setup instructions','Submit compressed ARC-to-cloud transfer','Check transfer status and logs'),title='PHASED summary cloud transfer')
 switch(as.character(choice),'1'=cloud_setup_R1(),'2'=cloud_submit_R1(),'3'=cloud_status_R1())
}
