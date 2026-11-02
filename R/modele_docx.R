# Construit les modèles Word (styles, logo en en-tête, numéro de page) sans édition manuelle.
construire_modele <- function(sortie, logo = NULL, police = "Calibri", couleur_titre = "1F4E79") {
  tmp <- tempfile("docx")
  dir.create(tmp)
  ref <- file.path(tmp, "ref.docx")
  pandoc <- rmarkdown::pandoc_exec()
  system2(pandoc, c("-o", shQuote(ref), "--print-default-data-file", "reference.docx"))
  ext <- file.path(tmp, "x")
  utils::unzip(ref, exdir = ext)
  lire <- function(f) paste(readLines(file.path(ext, f), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  ecrire <- function(txt, f) {
    con <- file(file.path(ext, f), open = "wb")
    writeBin(charToRaw(enc2utf8(txt)), con)
    close(con)
  }

  st <- lire("word/styles.xml")
  st <- gsub("<w:rFonts [^>]*/>", sprintf('<w:rFonts w:ascii="%1$s" w:hAnsi="%1$s" w:cs="%1$s" w:eastAsia="%1$s"/>', police), st)
  st <- gsub('<w:color w:val="(0F4761|4F81BD|345A8A|365F91|156082)"[^>]*/>', sprintf('<w:color w:val="%s"/>', couleur_titre), st)
  ecrire(st, "word/styles.xml")

  ns <- paste0('xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
               'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships" ',
               'xmlns:wp="http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing" ',
               'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" ',
               'xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture"')
  pied <- sprintf(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:ftr %s><w:p><w:pPr><w:jc w:val="right"/></w:pPr>',
                         '<w:r><w:fldChar w:fldCharType="begin"/></w:r><w:r><w:instrText xml:space="preserve"> PAGE </w:instrText></w:r>',
                         '<w:r><w:fldChar w:fldCharType="end"/></w:r></w:p></w:ftr>'), ns)
  ecrire(pied, "word/footer9.xml")
  rel <- lire("word/_rels/document.xml.rels")
  ajout <- '<Relationship Id="rIdPied9" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/footer" Target="footer9.xml"/>'
  refs <- '<w:footerReference w:type="default" r:id="rIdPied9"/>'
  ct <- lire("[Content_Types].xml")
  parts <- '<Override PartName="/word/footer9.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.footer+xml"/>'

  if (!is.null(logo)) {
    dir.create(file.path(ext, "word/media"), showWarnings = FALSE)
    file.copy(logo, file.path(ext, "word/media/logo9.png"), overwrite = TRUE)
    cx <- 1150000L
    cy <- round(cx * 78 / 182)
    tete <- sprintf(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><w:hdr %s><w:p><w:r><w:drawing>',
      '<wp:inline distT="0" distB="0" distL="0" distR="0"><wp:extent cx="%d" cy="%d"/><wp:docPr id="901" name="Logo" descr="Logo MSF"/>',
      '<a:graphic><a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture"><pic:pic>',
      '<pic:nvPicPr><pic:cNvPr id="901" name="logo9.png"/><pic:cNvPicPr/></pic:nvPicPr>',
      '<pic:blipFill><a:blip r:embed="rIdLogo9"/><a:stretch><a:fillRect/></a:stretch></pic:blipFill>',
      '<pic:spPr><a:xfrm><a:off x="0" y="0"/><a:ext cx="%d" cy="%d"/></a:xfrm><a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>',
      '</pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r></w:p></w:hdr>'), ns, cx, cy, cx, cy)
    ecrire(tete, "word/header9.xml")
    dir.create(file.path(ext, "word/_rels"), showWarnings = FALSE)
    ecrire(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?><Relationships xmlns="http://schemas.openxmlformats.org/package/2006/relationships">',
                  '<Relationship Id="rIdLogo9" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/image" Target="media/logo9.png"/></Relationships>'),
           "word/_rels/header9.xml.rels")
    ajout <- paste0(ajout, '<Relationship Id="rIdTete9" Type="http://schemas.openxmlformats.org/officeDocument/2006/relationships/header" Target="header9.xml"/>')
    refs <- paste0('<w:headerReference w:type="default" r:id="rIdTete9"/>', refs)
    parts <- paste0(parts, '<Override PartName="/word/header9.xml" ContentType="application/vnd.openxmlformats-officedocument.wordprocessingml.header+xml"/>')
    if (!grepl('Extension="png"', ct, fixed = TRUE)) parts <- paste0('<Default Extension="png" ContentType="image/png"/>', parts)
  }
  ecrire(sub("</Relationships>", paste0(ajout, "</Relationships>"), rel, fixed = TRUE), "word/_rels/document.xml.rels")
  ecrire(sub("</Types>", paste0(parts, "</Types>"), ct, fixed = TRUE), "[Content_Types].xml")

  doc <- lire("word/document.xml")
  marges <- '<w:pgSz w:w="11906" w:h="16838"/><w:pgMar w:top="1418" w:right="1134" w:bottom="1134" w:left="1134" w:header="567" w:footer="567" w:gutter="0"/>'
  doc <- gsub("<w:pgSz [^>]*/>|<w:pgMar [^>]*/>", "", doc)
  if (grepl("<w:sectPr[^>]*/>", doc)) {
    doc <- sub("<w:sectPr[^>]*/>", paste0("<w:sectPr>", refs, marges, "</w:sectPr>"), doc)
  } else {
    doc <- sub("(<w:sectPr[^>]*>)", paste0("\\1", refs, marges), doc)
  }
  ecrire(doc, "word/document.xml")

  dir.create(dirname(sortie), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(sortie)) file.remove(sortie)
  zip::zipr(normalizePath(sortie, mustWork = FALSE), list.files(ext, full.names = TRUE, all.files = TRUE, no.. = TRUE))
  invisible(sortie)
}
