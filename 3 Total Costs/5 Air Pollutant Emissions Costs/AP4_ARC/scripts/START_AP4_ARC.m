% NEW CODE — R1; identified 29 September 2026
% Start the original AP4 model on ARC and compare with the Python outputs.
% This script uses its own location; no Mac paths need editing.
ap4_root = fileparts(mfilename('fullpath'));
ap4_analysis = fullfile(ap4_root, 'AP4_Investigation_2026-09-20');
addpath(ap4_analysis);
diary(fullfile(ap4_analysis, 'ARC_MATLAB_run.log'));
fprintf('MATLAB version: %s\nStarted: %s\n', version, datestr(now));
try
    run_native_validation(ap4_root, ap4_analysis);
    fprintf('Native AP4 coefficient comparison completed: %s\n', datestr(now));
    fprintf('Results: %s\n', fullfile(ap4_analysis, 'native_validation'));
catch ap4_error
    fprintf('%s\n', getReport(ap4_error, 'extended', 'hyperlinks', 'off'));
    diary off
    rethrow(ap4_error);
end
diary off
