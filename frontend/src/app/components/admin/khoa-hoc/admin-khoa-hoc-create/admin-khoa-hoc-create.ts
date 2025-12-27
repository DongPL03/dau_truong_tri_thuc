import { CommonModule } from '@angular/common';
import { Component } from '@angular/core';
import { FormsModule } from '@angular/forms';
import Swal from 'sweetalert2';
import { Base } from '../../../base/base';
import { ResponseObject } from '../../../../responses/response-object';
import { ChuDe } from '../../../../models/chude';

@Component({
  selector: 'app-admin-khoa-hoc-create',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './admin-khoa-hoc-create.html',
  styleUrl: './admin-khoa-hoc-create.scss',
})
export class AdminKhoaHocCreate extends Base {
  dto: any = {
    tieu_de: '',
    mo_ta: '',
    hinh_anh: '',
    chu_de_id: 0,
    trang_thai: 'DRAFT',
    gia_mo_khoa: 0,
    thu_tu: 0,
    danh_sach_bo_cau_hoi: [],
  };

  chuDes: ChuDe[] = [];
  saving = false;

  ngOnInit(): void {
    this.loadChuDe();
  }

  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<ChuDe[]>) => {
        this.chuDes = res.data || [];
      },
      error: () => {
        Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error');
      },
    });
  }

  submit() {
    if (!this.dto.tieu_de || !this.dto.chu_de_id) {
      Swal.fire('Thiếu dữ liệu', 'Vui lòng nhập tiêu đề và chọn chủ đề', 'warning');
      return;
    }

    this.saving = true;
    this.khoaHocService.create(this.dto).subscribe({
      next: () => {
        this.saving = false;
        Swal.fire('Thành công', 'Tạo khóa học thành công', 'success').then(() => {
          this.router.navigate(['/admin/khoa-hoc']);
        });
      },
      error: (err) => {
        this.saving = false;
        const msg = err?.error?.message || 'Không thể tạo khóa học';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }
}
