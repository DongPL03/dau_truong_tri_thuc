/**
 * Model cho chuỗi đăng nhập
 */
export interface LoginStreakResponse {
  streak_hien_tai: number;
  ngay_trong_chu_ky: number;
  da_diem_danh_hom_nay: boolean;
  ngay_dang_nhap_cuoi: string | null;
  phan_thuong_hom_nay: RewardDetail | null;
  danh_sach_ngay: DayReward[];
  thong_bao: string;
}

export interface RewardDetail {
  gold: number;
  xp: number;
  vat_pham_ten: string | null;
  vat_pham_icon: string | null;
  so_luong_vat_pham: number;
}

export interface DayReward {
  ngay: number;
  gold: number;
  xp: number;
  co_vat_pham: boolean;
  mo_ta: string;
  da_nhan: boolean;
  la_hom_nay: boolean;
  co_the_nhan: boolean;
}
