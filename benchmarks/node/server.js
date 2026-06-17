// Idiomatic lean Node: Fastify (the popular fast framework, with a real router) +
// the built-in cluster module so all cores are used (Node is single-threaded; each
// worker shares the listen socket). Logging off, as in production.
import cluster from "node:cluster";
import os from "node:os";
import Fastify from "fastify";

const PORT = Number(process.env.PORT || 8080);

if (cluster.isPrimary) {
  const workers = os.cpus().length;
  for (let i = 0; i < workers; i++) cluster.fork();
} else {
  const app = Fastify({ logger: false });

  app.get("/plaintext", (req, reply) => {
    reply.header("content-type", "text/plain").send("Hello, World!");
  });

  app.get("/json", (req, reply) => {
    reply.header("content-type", "application/json").send('{"message":"Hello, World!"}');
  });

  app.get("/user/:id", (req, reply) => {
    reply.header("content-type", "text/plain").send(req.params.id);
  });

  app.listen({ port: PORT, host: "127.0.0.1" });
}
