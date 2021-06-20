import express from "express";

// TODO: config module
if (!process.env.PORT) {
  process.exit(1);
}

const PORT: number = parseInt(process.env.PORT as string, 10);

const app = express();

app.listen(PORT, () => {
  console.log(`Serving on port ${PORT}.`);
});
