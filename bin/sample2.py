#!/usr/bin/python

import click

@click.command()
@click.option('--message', '-M', help="message to print")
@click.option('-X',
              help="the X factor - no one really knows what this "
                   "value is for; use at your own risk!")
@click.option('--this-is-a-long-option',
              metavar='SUPER11',
              help="some super weird long variable name, no idea what it "
                   "can mean")
def hello():
    click.echo('Hello World!')

if __name__ == '__main__':
    hello()
