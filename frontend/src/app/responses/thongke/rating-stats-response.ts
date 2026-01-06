/**
 * Response DTO for a single rating stats item
 */
export interface RatingStatsItemResponse {
  id: number;
  tieu_de: string;
  tong_danh_gia: number;
  trung_binh_sao: number;
  nguoi_tao?: string;
}

/**
 * Response DTO for rating overview statistics
 */
export interface RatingOverviewStatsResponse {
  tong_danh_gia_bo_cau_hoi: number;
  tong_danh_gia_khoa_hoc: number;
  trung_binh_sao_bo_cau_hoi: number;
  trung_binh_sao_khoa_hoc: number;
  top_rated_bo_cau_hoi: RatingStatsItemResponse[];
  top_rated_khoa_hoc: RatingStatsItemResponse[];
  lowest_rated_bo_cau_hoi: RatingStatsItemResponse[];
  lowest_rated_khoa_hoc: RatingStatsItemResponse[];
  most_reviewed_bo_cau_hoi: RatingStatsItemResponse[];
  most_reviewed_khoa_hoc: RatingStatsItemResponse[];
}
