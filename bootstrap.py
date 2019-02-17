import digitalocean
import sys
from os import environ
from argparse import ArgumentParser

parser = ArgumentParser()
parser.add_argument("action")

args = parser.parse_args()

token = environ['DO_API_TOKEN']
prefix = environ['DO_DROPLET_PREFIX']

manager = digitalocean.Manager(token=token)
droplets = [droplet for droplet in manager.get_all_droplets() if droplet.name.startswith(prefix)]
if args.action == "on" and not droplets:
    print(f"No droplets matching {prefix} found", file=sys.stderr)
    sys.exit(1)
    
for droplet in droplets:
    if args.action == "on":
        droplet.power_on()
    else:
        droplet.shutdown()
