#!/usr/bin/env python

from argparse import ArgumentParser
from subprocess import check_call
from tempfile import mkdtemp
from shutil import rmtree
import io
from os import path

parser = ArgumentParser()
parser.add_argument(dest="dotoken", help="Digital Ocean API token")
parser.add_argument(dest="s3token", help="Digital Ocan Spaces access token")
parser.add_argument(dest="s3secret", help="Digital Ocan Spaces secret key")
parser.add_argument(dest="gltoken", help="GitLab CI runner registration token")

args = parser.parse_args()

tempdir = mkdtemp()
try:
    userdata = path.join(tempdir, "bootstrap-cloud-config.yml")
    with io.open(userdata, "w") as tempfile:
        tempfile.write(io.open(path.join(path.realpath(path.dirname(__file__)), "bootstrap-cloud-config.yml"), encoding="utf-8").read() % dict(
            DIGITALOCEAN_API_TOKEN=args.dotoken,
            GITLAB_CI_REGISTRATION_TOKEN=args.gltoken,
            S3_ACCESS_KEY=args.s3token,
            S3_SECRET_KEY=args.s3secret,
        ))

    name = "gitlab-ci-dispatch"
    try:
        check_call([
            "docker-machine", "ssh", name,
            "gitlab-runner", "unregister", "--all-runners"
        ])
    except:
        pass
    try:
        check_call([
            "docker-machine", "ssh", name,
            "docker-machine", "ls", "--quiet", "--format", ".", "|", 
            "xargs", "docker-machine", "rm", "--force"
        ])
    except:
        pass
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
        "--digitalocean-size=s-1vcpu-1gb",
        "--digitalocean-tags=gitlab-ci",
        "--digitalocean-userdata=%s" % userdata,
        name
    ])
finally:
    try:
        rmtree(tempdir)
    except:
        pass