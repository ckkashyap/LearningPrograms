package main

import (
	"bytes"
	"fmt"
	"mime/multipart"
	"net/http"
	"os"
	"io"
)

func Upload(tarball string, repoUrl string) (err error) {
	// Create buffer
	buf := new(bytes.Buffer) // caveat IMO dont use this for large files, \
	// create a tmpfile and assemble your multipart from there (not tested) 
	w := multipart.NewWriter(buf)
	// Create a form field writer for field label
	label, err := w.CreateFormField("label")
	if err != nil {
		fmt.Print("Here 0")
		return err
	}
	// Write label field
	label.Write([]byte("label here"))
	// Create a form field writer for field summary
	summary, err := w.CreateFormField("summary")
	if err != nil {
		fmt.Print("Here 1")
		return err
	}
	// Write summary field
	summary.Write([]byte("summary here"))
	// Create file field
	fw, err := w.CreateFormFile("upload", tarball)
	if err != nil {
		fmt.Print("Here 2")
		return err
	}
	fd, err := os.Open(tarball)
	if err != nil {
		fmt.Print("Here 3")
		return err
	}
	defer fd.Close()
	// Write file field from file to upload
	_, err = io.Copy(fw, fd)
	if err != nil {
		fmt.Print("Here 4")
		return err
	}
	// Important if you do not close the multipart writer you will not have a 
	// terminating boundry 
	w.Close()
	req, err := http.NewRequest("POST", repoUrl, buf)
	if err != nil {
		fmt.Print("Here 5")
		return err
	}
	req.Header.Set("Content-Type", w.FormDataContentType())
	req.SetBasicAuth("email@email.com", "password")

	client := &http.Client {}

	res, err := client.Do(req)
	if err != nil {
		fmt.Print("Here 6")
		return err
	}
	io.Copy(os.Stderr, res.Body) // Replace this with Status.Code check
	return err
}

func main() {

	e := Upload("p1.go","http://127.0.0.1:8080");
	fmt.Print(e)
}
