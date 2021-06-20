import { LanguageString } from "./common"

export type MetaLink = {
  type: "meta",
  slug: string;
}

export type ExternalLink = {
  type: "external";
  url: string;
  text: LanguageString;
}

export type Link = MetaLink | ExternalLink;
