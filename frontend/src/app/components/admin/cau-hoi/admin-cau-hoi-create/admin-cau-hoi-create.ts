import {Component, OnInit, ViewChild} from '@angular/core';
import {CommonModule} from '@angular/common';
import {FormsModule, NgForm} from '@angular/forms';

import {Base} from '../../../base/base';
import {ResponseObject} from '../../../../responses/response-object';
import Swal from 'sweetalert2';
import {CauHoiDTO} from '../../../../dtos/cau-hoi/cauhoi-dto';

@Component({
  selector: 'app-admin-cau-hoi-create',
  standalone: true,
  imports: [CommonModule, FormsModule],
  templateUrl: './admin-cau-hoi-create.html',
  styleUrl: './admin-cau-hoi-create.scss',
})
export class AdminCauHoiCreate extends Base implements OnInit {
  @ViewChild('form') form!: NgForm;

  model: CauHoiDTO = new CauHoiDTO();
  boCauHoiId!: number;
  selectedFile?: File;
  previewUrl?: string;
  submitting = false;
  hovering = false;

  ngOnInit(): void {
    // ⭐ Ở admin route mình vẫn dùng param 'id' giống user
    this.boCauHoiId = Number(this.route.snapshot.paramMap.get('id'));
    console.log('Bộ câu hỏi ID từ route:', this.boCauHoiId);
    this.model.bo_cau_hoi_id = this.boCauHoiId;
  }

  onLoaiNoiDungChange(): void {
    if (this.model.loai_noi_dung === 'VAN_BAN') {
      this.selectedFile = undefined;
      this.previewUrl = undefined;
      this.model.duong_dan_tep = '';
    }
  }

  onFileSelected(event: Event): void {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;

    const file = input.files[0];
    this.selectedFile = file;

    const reader = new FileReader();
    reader.onload = () => (this.previewUrl = reader.result as string);
    reader.readAsDataURL(file);

    this.model.duong_dan_tep = file.name;
  }

  onSubmit(form: NgForm): void {
    if (this.submitting) return;

    if (form.invalid) {
      Swal.fire('Cảnh báo', 'Vui lòng điền đủ thông tin bắt buộc', 'warning');
      return;
    }

    this.submitting = true;

    this.cauHoiService.create(this.model).subscribe({
      next: (res: ResponseObject) => {
        const created = res.data;
        if (!created?.id) {
          this.submitting = false;
          Swal.fire('Lỗi', 'Không tạo được câu hỏi', 'error');
          return;
        }

        // Nếu có file media & không phải văn bản → upload
        if (this.selectedFile && this.model.loai_noi_dung !== 'VAN_BAN') {
          const loai = this.model.loai_noi_dung as 'HINH_ANH' | 'AM_THANH' | 'VIDEO';
          this.cauHoiService.uploadMedia(created.id, this.selectedFile, loai).subscribe({
            next: () => {
              this.submitting = false;
              Swal.fire({
                icon: 'success',
                title: 'Tạo câu hỏi thành công',
                showConfirmButton: true,
                confirmButtonText: 'Thêm câu hỏi khác'
              }).then(() => {
                form.resetForm();
                this.previewUrl = undefined;
                this.selectedFile = undefined;
              });
              // 🔁 QUAY VỀ TRANG CHI TIẾT BỘ ADMIN
              this.router.navigate(['/admin/bo-cau-hoi', this.boCauHoiId]);
            },
            error: (err) => {
              this.submitting = false;
              Swal.fire(
                'Tạo câu hỏi thành công nhưng upload file thất bại',
                err.error?.message || '',
                'warning'
              ).then(() => {
                this.router.navigate(['/admin/bo-cau-hoi', this.boCauHoiId]);
              });
            }
          });
        } else {
          // Không có file media
          this.submitting = false;
          Swal.fire('Thành công', 'Đã thêm câu hỏi!', 'success').then(() => {
            this.router.navigate(['/admin/bo-cau-hoi', this.boCauHoiId]);
          });
        }
      },
      error: (err) => {
        this.submitting = false;
        Swal.fire('Lỗi', err.error?.message || 'Không thể tạo câu hỏi', 'error');
      }
    });
  }

  cancel(): void {
    this.router.navigate(['/admin/bo-cau-hoi', this.boCauHoiId]);
  }

  async removeSelectedFile(): Promise<void> {
    const result = await Swal.fire({
      title: 'Xác nhận xoá tệp?',
      text: 'Bạn có chắc muốn xoá tệp này khỏi câu hỏi?',
      icon: 'warning',
      showCancelButton: true,
      confirmButtonColor: '#d33',
      cancelButtonColor: '#3085d6',
      confirmButtonText: 'Xoá',
      cancelButtonText: 'Huỷ',
      background: '#fff',
      color: '#333'
    });

    if (result.isConfirmed) {
      this.previewUrl = '';
      this.selectedFile = undefined;
      const input = document.querySelector('input[type="file"]') as HTMLInputElement;
      if (input) input.value = '';
      await Swal.fire({
        icon: 'success',
        title: 'Đã xoá!',
        text: 'Tệp đã được xoá thành công.',
        timer: 1200,
        showConfirmButton: false
      });
    }
  }

}
