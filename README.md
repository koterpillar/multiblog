Multiblog
=========

This is a quasi-static blog engine supporting rendering in multiple languages
based on the user agent language.

It is currently not generic enough to be used for anything other than a
personal blog, but the goal is to fully separate the appearance control from
the engine.

Usage
-----

The compiled program expects to find all the content under `content/`
directory.

Running `multiblog` will start an HTTP server for the content. The following
environment variables can be given:

- `LISTEN_PORT` - the port to listen on, default 8000.
- `SITE_URL` - the site URL used for the links, default `http://localhost:8000`.

### Content

There are two types of content to display: articles and metas. Articles have a
publication date, and form the main stream of the blog. Metas are pages which
don't have a date, and are typically acessible from the header/footer menu.

All content is written in Markdown and is converted into HTML when rendering.
Each file must have the following attributes:

* Slug, a string to use in the URL.
* Language, for distinguishing between the translated versions of the same
  content.
* Date - required for articles only; the absense of a date makes the content
  into a meta.

Attributes can be specified by either:

* Including an attribute pair in the [YAML metadata block][yaml-metadata].
* For languages: ending a file or a directory name in `-la`, for example, `-en`
  for English.
* For dates: including in the file or directory name, for example,
  `content/2015-01-03-myarticle.md`.
* For slugs: if not explicitly specified in YAML metadata, the file name (or a
  directory name, starting from the most specific) is taken to be a slug, after
  taking out the parts of it parsed as a date or language.

Some elements of the blog other than content itself must also be translated.
The translations use the file `content/strings.yaml` which must contain hashes
of the following structure: string - language - translation.

The sidebar links are specified in `content/links.yaml`. They can point either
to metas or to any external address.

### Analytics

Google Analytics is supported for the site. The ID must be specified in
`content/analytics.yaml`.

### Cross-posting

The blog supports cross-posting the articles to an external service. In order to
do that, it needs the credentials of both the service and the user on that
service. The service credentials are provided in `content/services.yaml` and are
specific to each service.

The content can be posted to multiple accounts across one or more services.
Cross-posting settings are specified separately in `content/cross-posting.yaml`.
Each entry has the following format:

```yaml
- service: xxxxxxxx
  lang: xx
  # service-specific information
```

To obtain the authorization for cross-posting to a particular account on a
service, run `multiblog authorize <service>`.

To post the _latest_ article to every configured service, run `multiblog
cross-post`. Note that the `SITE_URL` must be set correctly so that the link in
the cross post is valid.

The following services are supported:

#### Twitter

The application must be registered
on [Twitter Application Management](https://apps.twitter.com/) and needs to have
"Read and Write" permissions. The service credentials are Consumer Key and
Consumer Secret; in `services.yaml`:

```yaml
twitter:
  consumer_key: XXXXXXXXXXXXXXXX
  consumer_secret: XXXXXXXXXXXXXXXX
```

For cross-posting to a Twitter account, an OAuth access token and secret must be
obtained, using `multiblog authorize twitter`, and put into `cross-posting.yaml`
like this:

```yaml
- service: twitter
  lang: en
  oauth_token: XXXXXXXXXXXXXXXX
  oauth_token_secret: XXXXXXXXXXXXXXXX
```

Examples
--------

All the examples assume `https://blog.example.com/` to be the root address of
the blog.

### Articles

This article spread across two files for two languages will be displayed under
the URL `https://blog.example.com/2015/01/03/myarticle`:

`content/2015-01-03/myarticle-en.md`:

```markdown
# My article

This is the article content in English.
```

`content/2015-01-03/myarticle-zh.md`:

```markdown
# 我的文章

这是该文章的中文内容。
```

Note that the slug, date and language extracted from the directory and the file
names are used to match the different translations of the article. Each of those
can be specified in YAML metadata instead:

`content/another-translation.md`:

```markdown
---
date: 2015-01-03
slug: myarticle
lang: ru
---

# Моя статья

Это содержимое статьи на русском.
```

### Meta

This file does not have a date (either in the name or in the metadata), so it
will be displayed under `https://blog.example.com/about`:

`content/about-en.md`

```markdown
# About the blog

This is the site introduction in English.
```

Slug and language can be specified in YAML metadata, thus this will be a
different language version of the above meta:

`content/meta-page-2.md`

```markdown
---
slug: about
lang: zh
---

# 关于博客

这是该网站的中文介绍。
```

### Strings

This is an example of string translation file, `content/strings.yaml`:

```yaml
home:
  en: Home
  zh: 首页
```

### Links

An example of links file, `content/links.yaml`:

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

An example of the analytics file, `content/analytics.yaml`:

```yaml
google: UA-12345678-9
```

[yaml-metadata]: http://johnmacfarlane.net/pandoc/demo/example9/pandocs-markdown.html#extension-yaml_metadata_block
