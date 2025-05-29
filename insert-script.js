#!/usr/bin/env node

const fs = require('node:fs')

const out = fs.openSync('elm-timelines.html', 'w')

const mainMinJs = fs.readFileSync('main.min.js', 'utf-8')

const indexHtml = fs.readFileSync('index.html', 'utf-8')
indexHtml.split('\n').forEach(line => {
  const i = line.indexOf('<script ')
  if (i >= 0) {
    fs.writeSync(out, ' '.repeat(i) + '<script>\n')
    fs.writeSync(out, ' '.repeat(i + 2) + mainMinJs + '\n')
  } else {
    fs.writeSync(out, line + '\n')
  }
})
