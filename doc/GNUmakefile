SEAPLUS_TOP = ..


DOCUTILS_TOP = $(SEAPLUS_TOP)

DOC_BASE_NAME := seaplus

# The current, operational version of Seaplus:
CURRENT_VERSION := 1.0

DOC_NAME := $(DOC_BASE_NAME)-$(CURRENT_VERSION)

OVERALL_DOCUMENT_SOURCE := $(DOC_NAME).rst


CURRENT_SEAPLUS_HTML := $(DOC_NAME).html

HTML_TARGET = $(CURRENT_SEAPLUS_HTML)


CURRENT_SEAPLUS_PDF := $(DOC_NAME).pdf

PDF_TARGET = $(CURRENT_SEAPLUS_PDF)


DOC_GENERATED_FILES := $(HTML_TARGET) $(PDF_TARGET)

DOC_FILES := $(DOC_GENERATED_FILES) seaplus-example.png



PROJECT_CSS := pygments-default.css,seaplus.css

PROJECT_ICON := seaplus-icon.png


WEB_CONTENT = *.html *.css *.png tmp-rst/*.pdf

# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(SEAPLUS_WEB_MIRROR_BRANCH)")



.PHONY: all doc full-doc html pdf clone-mirror           \
		export-doc export-to-official export-to-mirror   \
		info-web clean clean-doc


# Default do-nothing target:
doc:


all:
	@echo "  Run 'make full-doc' to generate the manual of the SEAPLUS-$(CURRENT_VERSION) layer."



full-doc: $(HTML_TARGET)


html: clean-doc $(HTML_TARGET)

pdf: clean-doc $(PDF_TARGET)


# Creates a separate repository for the GitHub pages branch:
# (please then remove all initial content of that branch)
#
clone-mirror:
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-Seaplus.git $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages


export-doc: clean-doc full-doc export-to-official export-to-mirror


export-to-official: $(DOC_GENERATED_FILES)
	@echo "   Exporting Seaplus documentation to official website"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/Seaplus/


export-to-mirror: $(DOC_GENERATED_FILES)
	@echo "   Exporting Seaplus documentation to mirror website in $(SEAPLUS_WEB_MIRROR_BRANCH)"
	@/bin/cp -f $(WEB_CONTENT) $(SEAPLUS_WEB_MIRROR_BRANCH) && cd $(SEAPLUS_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "Seaplus doc updated." && git push && git pull --ff-only


clean: clean-doc

clean-doc:
	-@/bin/rm -f *.aux *.log *.maf *.mtc* *.stc* *.tex *.toc \
	$(CURRENT_SEAPLUS_PDF) $(DOC_GENERATED_FILES)

info: info-doc info-web

info-doc:
	@echo "OVERALL_DOCUMENT_SOURCE = $(OVERALL_DOCUMENT_SOURCE)"
	@echo "DOC_FILES = $(DOC_FILES)"


info-web:
	@echo "HTML_TARGET = $(HTML_TARGET)"
	@echo "SP = $(SP)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "WEB_ROOT = $(WEB_ROOT)"
	@echo "SEAPLUS_WEB_MIRROR_BRANCH = $(SEAPLUS_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

# For MYRIAD_TOP:
include $(SEAPLUS_TOP)/GNUmakesettings.inc
