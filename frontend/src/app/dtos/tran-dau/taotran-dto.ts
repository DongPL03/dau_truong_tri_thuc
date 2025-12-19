export class TaoTranDauDTO {
  ten_phong: string;
  bo_cau_hoi_id: number;
  cong_khai: boolean;
  ma_pin?: string;
  gioi_han_nguoi_choi: number;
  gioi_han_thoi_gian_cau_giay: number;
  luat_tinh_diem: 'BASIC' | 'SPEED_BONUS';
  loai_tran_dau: 'CASUAL' | 'RANKED';


  constructor(data: Partial<TaoTranDauDTO>) {
    this.ten_phong = data.ten_phong ?? '';
    this.bo_cau_hoi_id = data.bo_cau_hoi_id ?? 0;
    this.cong_khai = data.cong_khai ?? true;
    this.ma_pin = data.ma_pin;
    this.gioi_han_nguoi_choi = data.gioi_han_nguoi_choi ?? 10;
    this.gioi_han_thoi_gian_cau_giay = data.gioi_han_thoi_gian_cau_giay ?? 30;
    this.luat_tinh_diem = data.luat_tinh_diem ?? 'BASIC';
    this.loai_tran_dau = data.loai_tran_dau ?? 'CASUAL';
  }
}
