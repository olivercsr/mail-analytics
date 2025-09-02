package main

import (
  "bytes"
  // "os"
  "io"
  "fmt"
  "strings"
  "encoding/xml"
  // "strconv"
	"net/http"
  "text/template"
  // "github.com/antchfx/xmlquery"
)

type existDb struct {
  uri string
  resultTag string
}

func (db existDb) init() existDb {
  db.resultTag = "result"

  return db
}

func NewExistDb(appcfg appConfig) existDb {
	db := existDb{}.init()

	db.uri = appcfg.existdbUri

	return db
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

type existdbResult[T any] struct {
  Items []T `xml:"result"`
}

// type existdbCountResultSet struct {
//   // XMLName xml.Name `xml:"exist:result"`
//   // XMLName xml.Name `xml:"existResult"`
//   Items []countResultItem `xml:"result"`
// }

// var xmlData = `
// <existResult>
//   <result>
//     <rowbegin>11</rowbegin>
//     <rowend>22</rowend>
//     <rowcount>33</rowcount>
//   </result>
//   <result>
//     <rowbegin>12</rowbegin>
//     <rowend>23</rowend>
//     <rowcount>34</rowcount>
//   </result>
// </existResult>
// `

func parseXml[T any](xmlStr string) ([]T, error) {
  fmt.Println("======================================" + xmlStr)

  var results existdbResult[T]
  if err := xml.Unmarshal([]byte(xmlStr), &results); err != nil {
    return []T{}, err
  }

  // var results countResultItem
  // xml.Unmarshal([]byte(xmlData), &results)
  fmt.Printf("parseXml result: %+v\n", results)

  // for _, item := range results.Items {
  //   fmt.Printf("item: %+v\n\n", item)
  // }

  return results.Items, nil
}

/*
func (db existDb) parseXml(xmlStr string) {
  // time.Sleep(5 * time.Second)

  fmt.Println("======================================" + xmlStr)

  // var album album
  // err := xml.Unmarshal([]byte(xmlData), &album)
  // if err != nil {
  //   fmt.Fprintf(os.Stderr, "Error unmarshalling XML: %v\n", err)
  //   return
  // }
  //
  // fmt.Println("--- Processed via Unmarshalling ---")
  // fmt.Printf("ID: %s, Title: %s, Artist: %s, Price: %.2f\n",
  //   album.ID, album.Title, album.Artist, album.Price)

  root, err := xmlquery.Parse(strings.NewReader(xmlStr))
  queryResults, err := xmlquery.QueryAll(root, fmt.Sprintf("//%s", db.resultTag))
  if err != nil {
    fmt.Fprintf(os.Stderr, "Error querying album xml: %v\n", err)
  } else {
    fmt.Printf("Xpath result: ")
    for _, node := range queryResults {
      fmt.Println("Level: " + strconv.Itoa(node.Level()))
      fmt.Println("Data: " + node.Data)
      fmt.Println("InnerText: " + node.InnerText())

      resultItem := countResultItem{}
      if rowbegin, err := strconv.Atoi(node.SelectElement("rowbegin").InnerText()); err == nil {
        resultItem.rowbegin = uint(rowbegin)
      // } else {
      //   panic(err)
      }
      if rowend, err := strconv.Atoi(node.SelectElement("rowend").InnerText()); err == nil {
        resultItem.rowend = uint(rowend)
      // } else {
      //   panic(err)
      }
      if rowcount, err := strconv.Atoi(node.SelectElement("rowcount").InnerText()); err == nil {
        resultItem.rowend = uint(rowcount)
      // } else {
      //   panic(err)
      }
      fmt.Printf("resultItem: %+v\n", resultItem)

      // childnode := node.FirstChild
      // for eof := (childnode == nil); !eof; eof = (childnode == nil) {
      //   if childnode.Type == xmlquery.ElementNode {
      //     fmt.Println("Child Level: " + strconv.Itoa(childnode.Level()))
      //     fmt.Println("Child Data: " + childnode.Data)
      //     fmt.Println("Child InnerText: " + childnode.InnerText())
      //     fmt.Println("Child Xml: " + childnode.OutputXML(true))
      //   }
      //
      //   childnode = childnode.NextSibling
      // }
    }
  }
}
*/

func doQuery(db existDb, tenant string, query string) (string, error) {
  buf := strings.NewReader(query)

	uri := fmt.Sprintf("%s/%s", db.uri, tenant)

  resp, err := http.Post(uri, "text/xml", buf)
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

func query[T any](db existDb, tenant string, name string, variables []map[string]any) ([]T, error) {
	q, err := renderXquery(name, variables)
	if err != nil {
		return []T{}, err
	}

	xmlStr, err := doQuery(db, tenant, q)
	if err != nil {
		return []T{}, err
	}

	results, err := parseXml[T](xmlStr)
	if err != nil {
		return []T{}, err
	}

	return results, nil
}

