import {Component, OnInit, ViewChild} from '@angular/core';
import {Base} from '../../base/base';
import {FormsModule, NgForm} from '@angular/forms';
import {CauHoiDTO} from '../../../dtos/cau-hoi/cauhoi-dto';
import {ResponseObject} from '../../../responses/response-object';
import Swal from 'sweetalert2';

@Component({
  selector: 'app-tao-moi-bo-cau-hoi',
  imports: [
    FormsModule
  ],
  templateUrl: './tao-moi-cau-hoi.html',
  styleUrl: './tao-moi-cau-hoi.scss',
  standalone: true
})
export class CauHoiCreate extends Base implements OnInit {
  @ViewChild('form') form!: NgForm;

  model: CauHoiDTO = new CauHoiDTO();
  boCauHoiId!: number;
  selectedFile?: File;
  previewUrl?: string;
  submitting = false;
  readonly luaChonList: ('A' | 'B' | 'C' | 'D')[] = ['A', 'B', 'C', 'D'];

  constructor() {
    super();
    this.model.do_kho = 'TRUNG_BINH';
    this.model.loai_noi_dung = 'VAN_BAN';
  }

  ngOnInit() {
    this.boCauHoiId = Number(this.route.snapshot.paramMap.get('id'));
    this.model.bo_cau_hoi_id = this.boCauHoiId;
  }

  onLoaiNoiDungChange() {
    if (this.model.loai_noi_dung === 'VAN_BAN') {
      this.selectedFile = undefined;
      this.previewUrl = undefined;
      this.model.duong_dan_tep = "";
    }
  }

  onFileSelected(event: Event) {
    const input = event.target as HTMLInputElement;
    if (!input.files?.length) return;
    const file = input.files[0];

    // Validate size (ví dụ 10MB)
    if (file.size > 10 * 1024 * 1024) {
      Swal.fire('Lỗi', 'File quá lớn (Max 10MB)', 'error');
      return;
    }

    this.selectedFile = file;
    const reader = new FileReader();
    reader.onload = () => this.previewUrl = reader.result as string;
    reader.readAsDataURL(file);
    this.model.duong_dan_tep = file.name;
  }

  onSubmit(form: NgForm) {
    if (this.submitting) return;
    if (form.invalid) {
      Swal.fire('Cảnh báo', 'Vui lòng điền đủ thông tin bắt buộc', 'warning').then(r => {
      });
      return;
    }
    this.submitting = true;
    this.cauHoiService.create(this.model).subscribe({
      next: (res: ResponseObject) => {
        const created = res.data;
        if (!this.model.noi_dung || !this.model.dap_an_dung) {
          Swal.fire({
            icon: 'warning',
            title: 'Thiếu thông tin',
            text: 'Vui lòng nhập nội dung câu hỏi và chọn đáp án đúng!',
            confirmButtonColor: '#FF754C'
          });
          return;
        }

        // Validate media
        if (this.model.loai_noi_dung !== 'VAN_BAN' && !this.selectedFile) {
          Swal.fire('Thiếu file', `Vui lòng tải lên ${this.model.loai_noi_dung === 'HINH_ANH' ? 'hình ảnh' : 'âm thanh'}`, 'warning');
          return;
        }
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
              });
              // quay về trang chi tiết bộ câu hỏi
              this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', this.boCauHoiId]).then(r => {
              });
            }
          });
        } else {
          this.submitting = false;
          Swal.fire('Thành công', 'Đã thêm câu hỏi!', 'success').then(r => {
          });
          this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', this.boCauHoiId]).then(r => {
          });
        }
      },
      error: (err) => {
        this.submitting = false;
        Swal.fire('Tạo thành công nhưng upload file thất bại', err.error?.message || '', 'warning')
          .then(() => this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', this.boCauHoiId]));
      }
    });
  }

  cancel() {
    this.router.navigate(['/bo-cau-hoi/chi-tiet-bo-cau-hoi', this.boCauHoiId]).then(r => {
    });
  }


  async removeSelectedFile() {
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
      color: '#333',
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
        showConfirmButton: false,
      });
    }
  }

}

