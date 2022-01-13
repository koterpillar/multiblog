Multiblog
=========

This is a quasi-static blog engine supporting rendering in multiple languages
based on the user agent language.

It is currently not generic enough to be used for anything other than a
personal blog, but the goal is to fully separate the appearance control from
the engine.

Usage
-----

Running `multiblog` will start an HTTP server for the content found under the
current directory. The following environment variables can be given:

- `PORT` - the port to listen on, default 8000.
- `SITE_URL` - the site URL used for the links, default `http://localhost:8000`.
- `CONTENT_DIRECTORY` - the directory to look for content in instead.

### Content

There are two types of content to display: articles and metas. Articles have a
publication date, and form the main stream of the blog. Metas are pages which
don't have a date, and are typically acessible from the header/footer menu.

All content is written in Markdown and is converted into HTML when rendering.
Both articles and metas have multiple language versions in separate Markdown
files in the same directory.

Articles directories are named `yyyy-mm-dd-slug`, where `yyyy-mm-dd` is the
publication date of the article and `slug` is unique, within a day, article
identifier that will be used in the URL.

Metas are stored in `meta` directory, and their directory names are taken
directly to be their slugs.

Individual Markdown files inside the directories are named after their content
language code, for example, `en.md`.

Metas support rendering as slideshows via [remark](https://remarkjs.com/). To
switch a meta to presentation mode, add `layout: presentation` to `options.yaml`
in the meta directory.

Metas can be exported as PDF or DOCX by adding `.pdf` or `.docx` to the URL. To
control the exported file name, add `exportSlug: AnotherName` to `options.yaml`
in the meta directory.

Some elements of the blog other than content itself must also be translated. The
translations use the file `strings.yaml` which must contain hashes of the
following structure: string - language - translation.

The sidebar links are specified in `links.yaml`. They can point either to metas
or to any external address.

### Analytics

Google Analytics is supported for the site. The ID must be specified in
`settings.yaml`.

Examples
--------

All the examples assume `https://blog.example.com/` to be the root address of
the blog.

### Articles

This article spread across two files for two languages will be displayed under
the URL `https://blog.example.com/2015/01/03/myarticle`:

`2015-01-03-myarticle/en.md`:

```markdown
# My article

This is the article content in English.
```

`2015-01-03-myarticle/zh.md`:

```markdown
# 我的文章

这是该文章的中文内容。
```

### Metas

This is an example of a meta that will be displayed under
`https://blog.example.com/about`:

`meta/about/en.md`

```markdown
# About the blog

This is the site introduction in English.
```

`meta/about/zh.md`

```markdown
# 关于博客

这是该网站的中文介绍。
```

### Strings

This is an example of string translation file, `strings.yaml`:

```yaml
home:
  en: Home
  zh: 首页
```

### Links

An example of links file, `links.yaml`:

```yaml
- page: about
- url: https://www.yandex.ru
  text:
    en: Yandex
    ru: Яндекс
- url: https://www.github.com
  text: GitHub
```

### Analytics

An example of the analytics file, `analytics.yaml`:

```yaml
google: UA-12345678-9
```
