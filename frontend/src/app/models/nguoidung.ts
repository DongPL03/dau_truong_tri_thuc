import {VaiTro} from './vaitro';

export interface Nguoidung {
  id: number;
  ten_dang_nhap: string;
  email: string;
  ten_hien_thi: string;
  avatar_url: string;
  ho_ten: string;
  dia_chi: string;
  vai_tro: VaiTro;
  is_active: boolean;
}
