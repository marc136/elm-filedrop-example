import './main.css';
import { Elm } from './Main.elm';

const main = Elm.Main.init({ node: document.getElementById('root') });

window.main = main

main.ports.drop.subscribe(files => {
  console.log(files);

  Array.from(files).forEach(file => {
    const reader = new FileReader()
    reader.onload = () => {
      const data = {
        dataUri: reader.result,
        name: file.name,
        size: file.size,
        type: file.type
      }

      console.log('sending file ', data.name, data.type)
      try {
        main.ports.getDataUri.send(data);
      } catch (ex) {
        console.error(ex)
      }
    }
    reader.onerror = console.error;
    reader.readAsDataURL(file);
  });
});

main.ports.logError.subscribe(console.error);

