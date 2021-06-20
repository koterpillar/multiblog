import { Article } from './article';
import { LanguageString } from './common';
import { Link } from './link';
import { Meta } from './meta';

export type Site = {
  articles: Article[];
  meta: Meta[];
  strings: Map<string, LanguageString>;
  links: Link[];
  // TODO: Rest of AppData
};
