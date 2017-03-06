library("curl")

curl_download(
  url = "https://www.data.gouv.fr/s/resources/listes-de-personnalites-issues-de-wikidata-1/20160905-165452/deputes.csv",
  destfile = "inst/extdata/table_deputes.csv"
  )

curl_download(
  url = "http://www.data.gouv.fr/fr/datasets/r/54aa0d23-d00e-4cdd-ba33-62299b64199d",
  destfile = "inst/extdata/DINSIC-Panorama_des_grands_projets_SI_20161116.xlsx"
  )
