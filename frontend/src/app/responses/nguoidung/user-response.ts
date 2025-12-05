import {VaiTro} from '../../models/vaitro';

export interface UserResponse {
  id: number;
  ten_dang_nhap: string;
  email: string;
  ho_ten?: string;
  dia_chi?: string;
  ten_hien_thi?: string;
  avatar_url?: string;
  is_active: boolean;
  is_xoa?: number;
  vai_tro?: VaiTro;
  tao_luc?: string; // ISO
}
