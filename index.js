const core = require('@actions/core');
const exec = require('@actions/exec');
const local_file_name = __dirname + '/elisp-check.el';

async function main() {
  try {
    // Get inputs
    const check = core.getInput('check');
    const file = core.getInput('file');
    const ignore = getBooleanInput('ignore_warnings')
    const as_errors = getBooleanInput('warnings_as_errors')

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
    process.exitCode = 1
  }
}

// Get a Boolean input as its Emacs Lisp equivalent
function getBooleanInput(name) {
  let input = core.getInput(name);

  if (input === 'true') {
    return 't';
  } else if (input === 'false') {
    return 'nil';
  } else {
    throw Error(`Option '${name}' could not be interpreted as a Boolean`);
  }
}

main();
