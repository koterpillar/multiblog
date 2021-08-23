import { Parser, Response, Route, URL, route } from 'typera-koa';

type Page = string;

type AppResponse = Response.Ok<Page> | Response.NotFound | Response.BadRequest<string>;

const listArticles: Route<AppResponse> = route
  .get('/')
  .handler(async (request) => {
    // TODO: list articles
    return Response.notFound();
  });

const viewArticle: Route<AppResponse> = route
  .get('/:year(int)/:month(int)/:day(int)/:slug')
  .handler(async (request) => {
    const date = new Date(request.routeParams.year, request.routeParams.month, request.routeParams.day);
    const slug = request.routeParams.slug;
    // TODO: get article
    return Response.notFound();
  });
