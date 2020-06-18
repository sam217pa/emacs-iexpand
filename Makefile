all: README.md iexpand.elc

README.md: make-readme-markdown.el iexpand.el
	emacs --script $< <iexpand.el 2>/dev/null \
	| gsed 's|^## iexpand.el|\n&|' > $@

make-readme-markdown.el:
	wget -q -O $@ https://raw.github.com/mgalgs/make-readme-markdown/master/make-readme-markdown.el

%.elc: %.el ; emacs -Q -f byte-compile-file --batch

# .INTERMEDIATE: make-readme-markdown.el
