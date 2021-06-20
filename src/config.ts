export type Config = {
  port: number;
  realSiteAddress: string | null;
};

export function fromEnvironment(): Config {
  const port: number = parseInt(process.env.LISTEN_PORT as string || "8000", 10);

  const realSiteAddress = process.env.SITE_ADDRESS || null;

  return { port, realSiteAddress };
}

export function siteAddress(config: Config): string {
  return config.realSiteAddress || `http://localhost:${config.port}`;
}
