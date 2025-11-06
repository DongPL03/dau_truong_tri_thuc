export interface TrandauResponse {
  id: number;
  ten_phong: string;
  ma_phong: string;
  cong_khai: boolean;
  ma_pin: string | null;
  gioi_han_nguoi_choi: number;
  gioi_han_thoi_gian_cau_giay: number;
  luat_tinh_diem: string;
  trang_thai: string;
  chu_phong_ten: string;
  bo_cau_hoi_id: number;
  bo_cau_hoi_tieu_de: string;
  tao_luc: string;        // LocalDateTime -> string ISO
  bat_dau_luc?: string;
  ket_thuc_luc?: string;
}
