const fs = require('fs');

const args = process.argv;
const mdFilePath = args[2];

const findMatches = (reg, str) => {
    const matches = [];

    let match;
    let isMatch = false;

    while (match = reg.exec(str)) {
        isMatch = true;
        matches.push(match);
    }

    return isMatch
        ? matches
        : null;
}

const regExp = /\$(.+?)\$/g;
const mdFileContent = fs.readFileSync(mdFilePath, 'utf-8');
const modifiedContent = mdFileContent.replace(regExp, '<span data-katex="$1"></span>');

fs.writeFileSync(mdFilePath, modifiedContent, 'utf-8');