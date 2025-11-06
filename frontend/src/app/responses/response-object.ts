export interface ResponseObject<T = any> {
  message?: string;
  status?: number | string; // BE dùng HttpStatus Enum
  data?: T | null;
}
