import fs from 'fs';

const configPath = './config.json';

export const loadConfig = () => {
  const rawData = fs.readFileSync(configPath);
  return JSON.parse(rawData);
}
