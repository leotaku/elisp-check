const core = require('@actions/core');
const exec = require('@actions/exec');
const fs = require('fs')

const local_file_name = '__github-action-elisp-check.el';
const elisp = fs.readFileSync(__dirname + '/elisp-check.el', 'utf-8')

async function main() {
  try {
    // Setup local elisp file
    fs.writeFileSync(local_file_name, elisp);

    // Get check name and execute
    const check = core.getInput('check');
    const file = core.getInput('files');
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
