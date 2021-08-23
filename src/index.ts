import Koa from "koa";

import { fromEnvironment, siteAddress } from './config';

const config = fromEnvironment();

const app = new Koa();

app.listen(config.port, () => {
  console.log(`Serving ${siteAddress(config)} on port ${config.port}.`);
});
