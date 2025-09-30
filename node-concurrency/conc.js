const fs = require('node:fs');
const fsPromises = require('node:fs/promises');

const f = () => {
  fs.readFile("conc.js", 'utf-8', (err, data) => {
    if (err) throw err;
      console.log(data);
      fs.readFile("conc.js", 'utf-8', (err, data) => {
        console.log(data);
      })
  });
};

async function g() {
  try {
    const data = await fsPromises.readFile('conc.js', { encoding: 'utf8' })
    console.log(data);
    const data2 = await fsPromises.readFile('conc.js', { encoding: 'utf8' })
    console.log(data2);
  } catch(err) {
    console.log(err);
  }
}

fsPromises
  .readFile('conc.js', { encoding: 'utf8' })
  .then((data) => new Promise(() => {console.log(data)}))


// g();
