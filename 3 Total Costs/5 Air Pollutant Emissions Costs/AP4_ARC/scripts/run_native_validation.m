% NEW CODE — R1; identified 29 September 2026
function run_native_validation(model_dir, analysis_dir)
% Run untouched downloaded MATLAB scripts, export outputs, and compare to Python.
% Example:
% run_native_validation('/Users/amirgazar/Downloads/AP4 Model', ...
% '/Users/amirgazar/Downloads/AP4 Model/AP4_Investigation_2026-09-20')
% Requires licensed MATLAB. This runner was prepared but not executed here.
old_dir = pwd;
cleanup = onCleanup(@() cd(old_dir));
native_dir = fullfile(analysis_dir, 'native_validation');
if ~exist(native_dir, 'dir'), mkdir(native_dir); end
cd(model_dir);
r = execute_original();
save(fullfile(native_dir, 'native_results.mat'), '-struct', 'r');
county = readtable(fullfile(model_dir,'AP4_Inputs','AP4_County_List.xlsx'));
egu = readtable(fullfile(model_dir,'AP4_Inputs','AP4_EGU_List.xlsx'));
egu = egu(string(egu.nei_2017)=="Yes",:);
assert(height(county)==size(r.MD_Ground,1));
assert(height(egu)==size(r.MD_EGU_Point,1));
assert(isequal(double(egu.row),(1:height(egu))'));
% The saved native workspace retains AP4's own identifier objects for inspection.
% Confirm their ordering against these external lists before publication.
types = {'ground','non_egu_point','egu_point'};
arrays = {r.MD_Ground,r.MD_Non_EGU_Point,r.MD_EGU_Point};
for k=1:3
    if k<3, ids=county; else, ids=egu; end
    t = [ids array2table(arrays{k},'VariableNames',{'NH3','NOx','PM25','SO2','VOC'})];
    writetable(t,fullfile(native_dir,[types{k} '_native_USD2020_per_metric_tonne.csv']));
    py=readtable(fullfile(analysis_dir,[types{k} '_as_downloaded_USD2020_per_metric_tonne.csv']), ...
        'VariableNamingRule','preserve');
    py_values=table2array(py(:,{'NH3','NOx','PM2.5','SO2','VOC'}));
    err=max(abs(arrays{k}(:)-py_values(:)));
    fprintf('%s maximum absolute Python/native difference: %.12g USD/tonne\n',types{k},err);
    assert(err<0.05,'Python/native mismatch exceeds $0.05 per tonne.');
end
end

function r = execute_original()
% Separate workspace because Load_Workspace.m calls clear.
run AP4_Control_Script
r = struct('MD_Ground',MD_Ground,'MD_Non_EGU_Point',MD_Non_EGU_Point, ...
    'MD_EGU_Point',MD_EGU_Point,'MD_Summary_Matrix',MD_Summary_Matrix, ...
    'AP4_County_List',AP4_County_List,'AP4_EGU_List',AP4_EGU_List, ...
    'wtp',wtp,'usd_year',usd_year,'wtp_year',wtp_year);
end
