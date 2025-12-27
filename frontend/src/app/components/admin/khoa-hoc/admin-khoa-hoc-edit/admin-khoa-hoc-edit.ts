import { CommonModule } from '@angular/common';
import { Component, OnInit } from '@angular/core';
import { FormsModule } from '@angular/forms';
import { Base } from '../../../base/base';
import { ResponseObject } from '../../../../responses/response-object';
import { ChuDe } from '../../../../models/chude';
import { KhoaHoiDetailResponse } from '../../../../responses/khoahoc/khoa-hoi-detail-response';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-admin-khoa-hoc-edit',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './admin-khoa-hoc-edit.html',
  styleUrl: './admin-khoa-hoc-edit.scss',
})
export class AdminKhoaHocEdit extends Base implements OnInit {
  id!: number;
  dto: any = {
    tieu_de: '',
    mo_ta: '',
    hinh_anh: '',
    chu_de_id: 0,
    trang_thai: 'DRAFT',
    gia_mo_khoa: 0,
    thu_tu: 0,
  };

  chuDes: ChuDe[] = [];
  loading = false;
  saving = false;

  ngOnInit(): void {
    this.id = Number(this.route.snapshot.paramMap.get('id'));
    this.loadChuDe();
    this.loadDetail();
  }

  loadChuDe() {
    this.chuDeService.getChuDe(0, 100).subscribe({
      next: (res: ResponseObject<ChuDe[]>) => {
        this.chuDes = res.data || [];
      },
      error: () => Swal.fire('Lỗi', 'Không thể tải danh sách chủ đề', 'error'),
    });
  }

  loadDetail() {
    this.loading = true;
    this.khoaHocService.getById(this.id).subscribe({
      next: (res: ResponseObject<KhoaHoiDetailResponse>) => {
        const kh = res.data?.khoa_hoc;
        if (kh) {
          this.dto = {
            tieu_de: kh.tieu_de,
            mo_ta: kh.mo_ta,
            hinh_anh: kh.hinh_anh,
            chu_de_id: kh.chu_de_id,
            trang_thai: kh.trang_thai,
            gia_mo_khoa: kh.gia_mo_khoa || 0,
            thu_tu: kh.thu_tu || 0,
          };
        }
        this.loading = false;
      },
      error: () => {
        this.loading = false;
        Swal.fire('Lỗi', 'Không thể tải chi tiết khóa học', 'error');
      },
    });
  }

  submit() {
    if (!this.dto.tieu_de || !this.dto.chu_de_id) {
      Swal.fire('Thiếu dữ liệu', 'Vui lòng nhập tiêu đề và chọn chủ đề', 'warning');
      return;
    }

    this.saving = true;
    this.khoaHocService.update(this.id, this.dto).subscribe({
      next: () => {
        this.saving = false;
        Swal.fire('Thành công', 'Cập nhật khóa học thành công', 'success').then(() => {
          this.router.navigate(['/admin/khoa-hoc']);
        });
      },
      error: (err) => {
        this.saving = false;
        const msg = err?.error?.message || 'Không thể cập nhật khóa học';
        Swal.fire('Lỗi', msg, 'error');
      },
    });
  }
}
