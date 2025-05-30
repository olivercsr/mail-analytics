package main

import (
  "bytes"
  "os"
  "io"
  "fmt"
  "strings"
	"net/http"
  "text/template"
  "github.com/antchfx/xmlquery"
)

type existDb struct {
  uri string
}

func NewExistDb(appcfg appConfig) existDb {
	return existDb {
		uri: appcfg.existdbUri,
	}
}

func renderXquery(name string, variables []map[string]any) (string, error) {
  tmpl := template.Must(template.ParseGlob("queries/**"))

  data := map[string][]map[string]any{
  	"variables": variables,
  }

  buf := new(bytes.Buffer)
  err := tmpl.ExecuteTemplate(buf, fmt.Sprintf("%s.xquery", name), data)
  if err != nil {
    return "", err
  }

  return buf.String(), nil
}

func parseXml(xmlData string) {
  // time.Sleep(5 * time.Second)

  /*
  var album album
  err := xml.Unmarshal([]byte(xmlData), &album)
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error unmarshalling XML: %v\n", err)
    return
  }

  fmt.Println("--- Processed via Unmarshalling ---")
  fmt.Printf("ID: %s, Title: %s, Artist: %s, Price: %.2f\n",
    album.ID, album.Title, album.Artist, album.Price)
  */

  root, err := xmlquery.Parse(strings.NewReader(xmlData))
  queryResults, err := xmlquery.QueryAll(root, "//rowcount")
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error querying album xml: %v\n", err)
  } else {
    fmt.Printf("Xpath result: ")
    for _, node := range queryResults {
      fmt.Println(node.InnerText())
    }
  }
}

func (db existDb) doQuery(query string) (string, error) {
  buf := strings.NewReader(query)

  resp, err := http.Post(db.uri, "text/xml", buf)
  if err != nil {
    panic(err)
  }
  defer resp.Body.Close()

  body, err := io.ReadAll(resp.Body)
  if err != nil {
    panic(err)
  }

  return string(body), nil
}

func (db existDb) query(name string, variables []map[string]any) ([]any, error) {
	q, err := renderXquery(name, variables)
	if err != nil {
		return []any{}, err
	}

	xmlStr, err := db.doQuery(q)
	if err != nil {
		return []any{}, err
	}

	parseXml(xmlStr)

	return []any{}, nil // TODO: implement
}

