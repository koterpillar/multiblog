// TODO: ISO codes or something as a library
export type Language = string;

export type LanguageMap<A> = Map<Language, A>;

export type LanguageString = LanguageMap<String>;

// TODO HTML, Markdown, etc.
export type Content = string;

export type LanguageContent = LanguageMap<Content>;
