export type Config = {
  port: number;
  realSiteAddress: string | null;
  directory: string;
};

export function fromEnvironment(): Config {
  const port: number = parseInt(process.env.LISTEN_PORT as string || "8000", 10);

  const realSiteAddress = process.env.SITE_ADDRESS || null;

  const directory = process.env.CONTENT_DIRECTORY || process.cwd();

  return { port, realSiteAddress, directory };
}

export function siteAddress(config: Config): string {
  return config.realSiteAddress || `http://localhost:${config.port}`;
}
