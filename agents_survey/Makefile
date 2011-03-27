IN=.:../roles_bib:

all:
	TEXINPUTS=$(IN) BIBINPUTS=$(IN) rubber --pdf --force survey

clean:
	rubber --pdf --clean survey
