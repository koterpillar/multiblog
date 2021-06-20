import express from "express";

import { fromEnvironment, siteAddress } from './config';

const config = fromEnvironment();

const app = express();

app.listen(config.port, () => {
  console.log(`Serving ${siteAddress(config)} on port ${config.port}.`);
});
