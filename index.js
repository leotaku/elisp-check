const core = require('@actions/core');
const exec = require('@actions/exec');

async function run() {
  try {
    const check = core.getInput('checks');

    core.debug((new Date()).toTimeString())
    await exec.exec('emacs', [ '--no-site-file', '--batch', '--eval', `(message "${check}")` ])
    await exec.exec('ls')
    core.debug((new Date()).toTimeString())

    core.setOutput('time', new Date().toTimeString());
  }
  catch (error) {
    core.setFailed(error.message);
  }
}

run()
