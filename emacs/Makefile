all:
	# create the Emacs configuration directory
	mkdir -p ~/.emacs.d
	# copy the configuration files
	cp early-init.el ~/.emacs.d/early-init.el
	cp init.el ~/.emacs.d/init.el
	# create the paths configuration file based on the current directory
	echo ";; paths.el -- Paths configuration for Emacs" > ~/.emacs.d/paths.el
	echo "(setq nxt/custom-features-directory \"$(CURDIR)/features/\")" >> ~/.emacs.d/paths.el

.PHONY: clean
clean:
	@echo "This will remove all configurations and caches. Are you sure? [y/N]" && read ans && [ $${ans:-N} = y ]
	rm -rf ~/.emacs.d
	rm -rf ~/.cache/emacs
