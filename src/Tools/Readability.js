import { Readability } from "@mozilla/readability";
import { JSDOM } from 'jsdom';

export const loadReadabilityFromUrl_ = ({ Nothing, Just }) => (url) => async () => {
  const response = await fetch(url, { 'method': 'GET' });
  if (!response.ok) return Nothing;
  const text = await response.text();
  const doc = new JSDOM(text, { url });
  return Just(new Readability(doc.window.document));
}

export const parse_ = ({ Nothing, Just }) => (readability) => {
  const article = readability.parse();
  if (article === null) return Nothing;
  return Just(article);
}

