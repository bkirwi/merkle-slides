index.html: template.revealjs slides.md
	pandoc -t revealjs --standalone --template template.revealjs -o index.html slides.md
