#include <stdlib.h>
#include <zlib.h>

/* The zlib package only offers no-flush and finish. An XLSX sheet is built from
   segments that each end with a full flush, so they concatenate into one raw
   deflate stream; this is the smallest wrapper that exposes that. */

z_stream *smart_csv_deflate_new(int level)
{
    z_stream *stream = calloc(1, sizeof(z_stream));
    if (stream == NULL) {
        return NULL;
    }
    /* Negative window bits: raw deflate, as a ZIP entry stores it. */
    if (deflateInit2(stream, level, Z_DEFLATED, -15, 8, Z_DEFAULT_STRATEGY) != Z_OK) {
        free(stream);
        return NULL;
    }
    return stream;
}

int smart_csv_deflate(
    z_stream *stream,
    const unsigned char *input,
    unsigned int input_length,
    int flush,
    unsigned char *output,
    unsigned int output_capacity,
    unsigned int *input_used,
    unsigned int *output_used)
{
    stream->next_in = (Bytef *)input;
    stream->avail_in = input_length;
    stream->next_out = output;
    stream->avail_out = output_capacity;
    int status = deflate(stream, flush);
    *input_used = input_length - stream->avail_in;
    *output_used = output_capacity - stream->avail_out;
    return status;
}

void smart_csv_deflate_free(z_stream *stream)
{
    deflateEnd(stream);
    free(stream);
}
