all: index.html

index.html: template.revealjs slides.md override.css
	pandoc -t revealjs \
		--standalone \
		--filter filter/dot.hs \
		--no-highlight \
		--css override.css \
		--template default.revealjs \
		-V theme=moon \
		-V transition=slide \
		-o index.html \
		slides.md
