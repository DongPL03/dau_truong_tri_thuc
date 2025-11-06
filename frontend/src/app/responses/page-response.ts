export interface PageResponse<T> {
  items: T[];             // ✅ phải là items (khớp với backend)
  currentPage: number;
  totalPages: number;
  totalItems: number;
  pageSize: number;
}
