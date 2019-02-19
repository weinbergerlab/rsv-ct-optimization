#!/usr/bin/env python

from argparse import ArgumentParser
from subprocess import check_call
from tempfile import mkdtemp
from shutil import rmtree
import io
from os import path

parser = ArgumentParser()
parser.add_argument(dest="dotoken", help="Digital Ocean API token")
parser.add_argument(dest="gltoken", help="GitLab CI runner registration token")

args = parser.parse_args()

tempdir = mkdtemp()
try:
    userdata = path.join(tempdir, "bootstrap-cloud-config.yml")
    with io.open(userdata, "w") as tempfile:
        tempfile.write(io.open(path.join(path.realpath(path.dirname(__file__)), "bootstrap-cloud-config.yml"), encoding="utf-8").read() % dict(
            DIGITALOCEAN_API_TOKEN=args.dotoken,
            GITLAB_CI_REGISTRATION_TOKEN=args.gltoken,
        ))

    name = "gitlab-ci-dispatch"
    try:
        check_call([
            "docker-machine", "rm", 
            "--force", 
            name
        ])
    except:
        pass

    io.open("/tmp/foo", "w").write(io.open(userdata).read())
    check_call([
        "docker-machine", "create", 
        "--driver=digitalocean", 
        "--digitalocean-access-token=%s" % args.dotoken,
        "--digitalocean-image=ubuntu-16-04-x64",
        "--digitalocean-monitoring",
        "--digitalocean-region=sfo2",
        "--digitalocean-size=s-1vcpu-2gb",
        "--digitalocean-tags=gitlab-ci",
        "--digitalocean-userdata=%s" % userdata,
        name
    ])
finally:
    try:
        rmtree(tempdir)
    except:
        pass