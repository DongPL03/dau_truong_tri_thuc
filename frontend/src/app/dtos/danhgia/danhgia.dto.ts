export interface DanhGiaResponse {
  id: number;
  nguoi_dung_id: number;
  nguoi_dung_ten: string;
  nguoi_dung_avatar: string | null;
  loai_doi_tuong: 'BO_CAU_HOI' | 'KHOA_HOC';
  doi_tuong_id: number;
  so_sao: number;
  noi_dung: string | null;
  tao_luc: string;
  cap_nhat_luc: string;
}

export interface DanhGiaStatsResponse {
  tong_danh_gia: number;
  trung_binh_sao: number;
  phan_bo_sao: { [key: number]: number };
}

export interface CreateDanhGiaRequest {
  loai_doi_tuong: 'BO_CAU_HOI' | 'KHOA_HOC';
  doi_tuong_id: number;
  so_sao: number;
  noi_dung?: string;
}

export interface PageResponse<T> {
  content: T[];
  totalElements: number;
  totalPages: number;
  size: number;
  number: number;
  first: boolean;
  last: boolean;
}
