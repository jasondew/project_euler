#include <stdio.h>
// #include <cutil.h>

#define MAX 1000000
#define MAX_ITERATIONS 1000

#define CUDA_SAFE_CALL(x) x

__global__ void kernel(int* a) {
  int idx = blockIdx.x * blockDim.x + threadIdx.x;

  int i = 0;
  unsigned int answer = idx;

  if (idx != 0 && idx <= MAX) {
    while (answer != 1 && i < MAX_ITERATIONS) {
      if ((answer & 1) == 0) { answer = answer >> 1; }
      else { answer = 3 * answer + 1; }
      i++;
    }
  }

  if (i == MAX_ITERATIONS) {
    a[idx] = 69696969;
  } else {
    a[idx] = i;
  }
}

int main() {
  int threads_per_block = ceil(MAX / 256.0);
  printf("threads per block = %d\n", threads_per_block);
  int dimx = threads_per_block*256;
  int num_bytes = dimx * sizeof(int);

  int *d_a = 0, *h_a = 0;

  h_a = (int*)malloc(num_bytes);
  CUDA_SAFE_CALL(cudaMalloc((void**)&d_a, num_bytes));

  if (0==h_a || 0==d_a) {
    printf("can't allocate memory");
  }

  CUDA_SAFE_CALL(cudaMemset(d_a, 0, num_bytes));
  CUDA_SAFE_CALL(cudaMemcpy(d_a, h_a, num_bytes, cudaMemcpyHostToDevice));

  cudaEvent_t start, stop;
  CUDA_SAFE_CALL(cudaEventCreate(&start); cudaEventCreate(&stop));

  CUDA_SAFE_CALL(cudaEventRecord(start, 0));
  kernel<<<threads_per_block, 256>>>(d_a);
  CUDA_SAFE_CALL(cudaEventRecord(stop, 0));


  CUDA_SAFE_CALL(cudaEventSynchronize(stop));
  float et;
  CUDA_SAFE_CALL(cudaEventElapsedTime(&et, start, stop));

  CUDA_SAFE_CALL(cudaEventDestroy(start)); CUDA_SAFE_CALL(cudaEventDestroy(stop));

  printf("kernel execution time: %8.6fms\n", et);

  CUDA_SAFE_CALL(cudaMemcpy(h_a, d_a, num_bytes, cudaMemcpyDeviceToHost));

  int max = 0;
  for(int i=0; i<dimx; i++) {
//    printf("%d ", h_a[i]);
    if (h_a[i] > max) max = h_a[i];
  }
//  printf("\n");
  printf("max is %d\n", max);

  free(h_a);
  CUDA_SAFE_CALL(cudaFree(d_a));

  return 0;
} 
