import { Elm } from './src/Main.elm';
import 'elm-canvas';
import data from './particleSystemData.json';

Elm.Main.init({
  flags: data,
  // flags: './particleSystemData.json',
  node: document.querySelector('main')
});
