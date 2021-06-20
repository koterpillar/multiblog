import { Language, LanguageMap } from './common';

export type LanguagePreference = LanguageMap<number>;

export const defaultLanguage: Language = "en";

export function singleLanguage(language: Language): LanguagePreference {
  return new Map([[language, 1]]);
}

export function matchLanguage<A>(preferences: LanguagePreference, values: LanguageMap<A>): A | null {
  let best: A | null = null;
  let bestScore: number | null = null;
  for (const [language, value] of values.entries()) {
    const score = preferences.get(language) || 0;
    if (bestScore === null || score > bestScore) {
      best = value;
      bestScore = score;
    }
  }
  return best;
}
