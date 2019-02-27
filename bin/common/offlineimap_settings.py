#!/usr/bin/python
# -*- coding: utf-8 -*-

"""
Set/get passwords for MSMTP or MPOP in Gnome Keyring

Copyright (C) 2009 Gaizka Villate
              2010 Emmanuel Bouthenot

Original author: Gaizka Villate <gaizkav@gmail.com>
Other author(s): Emmanuel Bouthenot <kolter@openics.org>

URL: http://github.com/gaizka/misc-scripts/tree/master/msmtp

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version.  See http://www.gnu.org/copyleft/gpl.html for
the full text of the license.
"""

import sys, os.path, optparse, getpass

try:
    import gnomekeyring as gk
except ImportError:
    print """Unable to import gnome keyring module
On Debian like systems you probably need to install the following package(s):
python-gnomekeyring"""
    sys.exit(-1)

class keyringManager():

    def __init__(self):
        self.app = 'msmtp'
        self.protocol = 'smtp'
        self.user= "andrej.skvortzov@gmail.com"
        self.server= "smtp.gmail.com"
        # get default keyring name
        try:
            self.keyring = gk.get_default_keyring_sync()
        except gk.NoKeyringDaemonError:
            print "ERR: can't open gnome keyring"
            print "Are you running this program under a GNOME session ?"
            sys.exit(-1)

    def get_app(self):
        return self.app

    def get_protocol(self):
        return self.protocol

    def set(self, password):
        # display name for password.
        display_name = '%s password for %s at %s' % (self.get_app().upper(), self.user, self.server)

        # select type. if you want some kind of "network" password, it seems that
        # appropriate type is network_password because it has a schema already.
        type = gk.ITEM_NETWORK_PASSWORD

        usr_attrs = {'user':self.user, 'server':self.server, 'protocol':self.get_protocol()}

        # Now it gets ready to add into the keyring. Do it.
        # Its id will be returned if success or an exception will be raised
        id = gk.item_create_sync(self.keyring, type, display_name, usr_attrs, password, False)
        return id is not None

    def get(self, user, server):
        protocol = self.get_protocol()
        try:
            results = gk.find_network_password_sync(user=user, server=server, protocol=protocol)
        except gk.NoMatchError:
            return None

        return results[0]["password"]

    def getpass(self):
        passwd = self.get(self.user, self.server )
        return passwd


def get_password():
    km = keyringManager()
    passwd = km.getpass()
    if passwd is None:
        print "No password found in a gnome keyring"
    return passwd

def set_password():
    km = keyringManager()
    import getpass
    password = getpass.getpass("Enter password:")
    password_confirmation = getpass.getpass("Confirm password: ")
    if password != password_confirmation:
        print "Error: password confirmation does not match"
        sys.exit(1)
    km.set(password)

def main():
    import sys
    import getpass
    if len(sys.argv) > 1 and sys.argv[1] == "--set":
        set_password()
        sys.exit(0)

    ret = True
    passwd = get_password()
    if passwd is None:
        ret = False
    else:
        print passwd
    return ret

if __name__ == '__main__':
    if main():
        sys.exit(0)
    else:
        sys.exit(-1)
