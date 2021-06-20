import { LanguageContent } from './common';

export type Article = {
  slug: string;
  content: LanguageContent;
  authored: Date;
};
