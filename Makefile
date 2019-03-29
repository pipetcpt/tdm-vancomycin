run:
	Rscript -e "shiny::runApp()"

deploy:
	Rscript rsconnect.R
