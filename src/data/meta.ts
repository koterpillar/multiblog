import { LanguageContent } from './common';

export type Layout = "default" | "presentation";

export type Meta = {
  slug: string;
  exportSlugOverride: string | null;
  layout: Layout;
  content: LanguageContent;
};
