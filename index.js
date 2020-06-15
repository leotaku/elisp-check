const core = require('@actions/core');
const exec = require('@actions/exec');
const local_file_name = __dirname + '/elisp-check.el';

async function main() {
  try {
    // Get inputs
    const check = core.getInput('check');
    const file = core.getInput('file');
    const ignore = bool_to_elisp(
      core.getInput('ignore_warnings')
    );
    const as_errors = bool_to_elisp(
      core.getInput('warnings_as_errors')
    );

    // Execute Emacs checks
    await exec.exec(
      'emacs',
      [
        '--no-site-file',
        '--batch',
        '--eval', '(setq debug-on-error t)',
        '--load', local_file_name,
        '--eval', `(setq elisp-check-ignore-warnings ${ignore})`,
        '--eval', `(setq elisp-check-warnings-as-errors ${as_errors})`,
        '--eval', `(elisp-check-run "${check}" "${file}" t)`
      ]
    );
  }
  catch (error) {
    core.setFailed(error.message);
  }
}

// Convert a Boolean to its Emacs Lisp equivalent
function bool_to_elisp(bool) {
  if (bool) {
    return 't';
  } else {
    return 'nil';
  }
}

main();
