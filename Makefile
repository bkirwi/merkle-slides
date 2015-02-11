all: index.html

index.html: template.revealjs slides.md
	pandoc -t revealjs \
		--standalone \
		--self-contained \
		--filter filter/dot.hs \
		--no-highlight \
		--css override.css \
		--template default.revealjs \
		-V theme=moon \
		-V transition=slide \
		-o index.html \
		slides.md
