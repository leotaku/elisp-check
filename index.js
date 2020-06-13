const core = require('@actions/core');
const exec = require('@actions/exec');
const local_file_name = __dirname + '/check-elisp.el';

async function main() {
  try {
    // Get check name and execute
    const check = core.getInput('check');
    const file = core.getInput('files');
    await exec.exec('ls', [ __dirname ]);
    await exec.exec(
      'emacs',
      [
        '--no-site-file',
        '--batch',
        '--load', local_file_name,
        '--eval', `(elisp-check-install "${check}")`,
        '--eval', `(elisp-check-run "${check}" "${file}")`
      ]
    );
  }
  catch (error) {
    core.setFailed(error.message);
  }
}

main()
