package main

import (
	// "encoding/xml"
)

type countResultItem struct {
	// XMLName xml.Name `xml:"result"`
	Begin string `xml:"rowbegin"`
	End string `xml:"rowend"`
	Count uint `xml:"rowcount"`
}

type rowCountResultItem struct {
	// TODO: implement fields
	// XMLName xml.Name `xml:"result"`
	Begin string `xml:"rowbegin"`
	End string `xml:"rowend"`
	Count uint `xml:"rowcount"`
}
