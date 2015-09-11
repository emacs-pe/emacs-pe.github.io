`vagrant.el' provides easy interaction with [vagrant][] also offers
a `vagrant' TRAMP method to interact with vagrant machines.

Installation:

To use this `vagrant.el' it's necessary to install the [vagrant-info][] plugin.

Usage:

You can get a complete list of your available vagrant machines with
`M-x vagrant-list-machines`.

The `vagrant' TRAMP method works by using an custom ssh_config(5) file
(`vagrant-ssh-config-file') for vagrant machines, so you need to add manually
the ssh-config of a machine with `M-x vagrant-add-ssh-config`.

Troubleshooting:

+ **My machine doesn't shows up**

  `vagrant.el' uses [vagrant-info][] and this plugin uses `global-status',
  so if the command `vagrant global-status` doesn't shows the information of
  your vagrant machine you can follow the instructions described in
  https://docs.vagrantup.com/v2/cli/global-status.html

+ **The `vagrant' TRAMP method show already deleted machines**

  You need to execute `M-x vagrant-tramp-cleanup-ssh-config` to cleanup the
  vagrant ssh-config.

[vagrant]: http://www.vagrantup.com/ "Vagrant"
[vagrant-info]: https://github.com/marsam/vagrant-info "vagrant-info plugin"
