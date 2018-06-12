# Emacs-crowi -  Crowi Emacs client.  [![][license-badge]][license-link]
========
* Emacs client for [Crowi](http://site.crowi.wiki/).
* Crowi API is experimental.

## Installation
* Add crowi.el to your Emacs load-path.
* Add basic config to your Emacs init file.
``` elisp
(require 'crowi)
(setq crowi-access-token "xxxxxxxxxxxxxxx") ;User setting -> API settings
(setq crowi-user "crowi username") ;default (getenv "USER")
(setq crowi-uri "http://hogehoge.com:3000") ;default http://localhost:3000
```

## Dependencies
* curl is required.

## Supported API
* usres.list
* pages.list pages.get pages.update pages.create
* comments.get comments.add
* attachments.list attachments.add attachments.remove

## Usage

* You can download Crowi's markdown, update it for editing, and add
comments and attachment from Emacs.


| command                | description|
|:-------------------|:--------------------------------------------------------|
| `crowi-users-list` | Output user list(username,email) to the newly created buffer.  |
| `crowi-pages-list` | Output page-list to the newly created buffer.  |
| `crowi-pages-get`  | Output the specified page to the newly created buffer |
| `crowi-pages-update-from-buffer` | Update page from buffer contents. |
| `crowi-pages-update-from-file` | Update page from file. |
| `crowi-pages-create-from-buffer` | Create page from buffer contents. |
| `crowi-pages-create-from-file` | Create page from file. |
| `crowi-comments-get` | Output comments to the newly created buffer. |
| `crowi-comments-add-from-buffer` | Add comment from buffer contents.|
| `crowi-attachments-download` | Download attachment of specified page.|
| `crowi-attachments-add` | Add attachment to specified page.|
| `crowi-attachments-remove` | Remove attachment to specified page.|

### Completion candidate

* In the situation where you need to specify a Crowi path or file, use
completing-read for display candidates. So recommend using helm or
ido. (It is not mandatory.)

* When updating Crowi page, adding a comment, etc., obtain the page
list from the server and display it as a candidate.

* When crowi-pages-create you should specify the path of Crowi.
This client asks for parent path and page name respectively.
(At least in my case) because I often want to add markdown somewhere in the
existing path.

* In addition to displaying the existing path as a candidate,
"/user/username/memo/YY/MM/DD" is also displayed as a candidate
when displaying the candidate of the creation destination.
This is an imitation of the behavior when Crowi's 'New' button is pressed.

* When creating a page from a file, add the filename(basename) is displayed as a
candidate.

### Save acquired page and attachments

* if you set `crowi-markdown-save` variable to t,
will save the Markdown file to `crowi-markdown-save-path`.

* When saving, keep the path name of Crowi.

```
Example:
Crowi path: /user/taro/memo/hogehoge
crowi-markdown-save-path: ~/wiki

download path -> ~/wiki/user/taro/memo/hogehoge.md
```
* Attachments are saved with the page name prefixed.

```
Example:
/user/taro/memo/hogehoge attachment image.png
crowi-markdown-save-path: ~/wiki

download path -> ~/wiki/user/taro/memo/hogehoge_image.png
```

### Customizable variables

| variable                | type | default |      |
|:------------------------|:----:|:--------|:-----|
| `crowi-uri`             | string | http://localhost:3000 | crowi server uri |
| `crowi-access-token`    | string | none | crowi access token |
| `crowi-user`            | string | getenv "USER" | crowi user name |
| `crowi-curl`            | string | curl | Executable for curl command. |
| `crowi-curl-options`    | string | none | Curl command additional options. Separate each option with a space. Like `-k -4`|
| `crowi-markdown-save`   | boolean | nil | Whether to save the Get Markdown in file. |
| `crowi-markdown-save-path` | string | ~/wiki | Path for Markdown file save. |
| `crowi-attachment-save-path` | string | ~/wiki | Path for attachment file save. |

### Assign key
If you want to assign Key in markdown-mode, please do as follows

``` elisp
(add-hook 'markdown-mode-hook
		  (lambda () (local-set-key "\C-cu" 'crowi-pages-update-from-file)))
```

## License
MIT

[license-badge]: http://img.shields.io/badge/license-MIT-blue.svg?style=flat-square
[license-link]: https://github.com/hirocarma/blob/master/LICENSE
