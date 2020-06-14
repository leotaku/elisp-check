const core = require('@actions/core');
const exec = require('@actions/exec');
const local_file_name = __dirname + '/elisp-check.el';

async function main() {
  try {
    // Get inputs and execute
    const check = core.getInput('check');
    const file = core.getInput('file');
    await exec.exec(
      'emacs',
      [
        '--no-site-file',
        '--batch',
        '--eval', '(setq debug-on-error t)',
        '--load', local_file_name,
        '--eval', `(elisp-check-run "${check}" "${file}" t)`
      ]
    );
  }
  catch (error) {
    core.setFailed(error.message);
  }
}

main()
